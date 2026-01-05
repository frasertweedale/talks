terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

provider "aws" {
  region = var.region
}

# --- NETWORK (Shared Infrastructure) ---
resource "aws_vpc" "workshop_vpc" {
  cidr_block           = "10.0.0.0/16"
  enable_dns_support   = true
  enable_dns_hostnames = true
  tags = { Name = "PKI-Workshop-VPC" }
}

resource "aws_internet_gateway" "gw" {
  vpc_id = aws_vpc.workshop_vpc.id
}

resource "aws_subnet" "main" {
  vpc_id                  = aws_vpc.workshop_vpc.id
  cidr_block              = "10.0.0.0/16" # Flat subnet for simplicity
  map_public_ip_on_launch = true
}

resource "aws_route_table" "rt" {
  vpc_id = aws_vpc.workshop_vpc.id
  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.gw.id
  }
}

resource "aws_route_table_association" "a" {
  subnet_id      = aws_subnet.main.id
  route_table_id = aws_route_table.rt.id
}

# --- PER-ENVIRONMENT RESOURCES ---

# 1. Unique SSH Key per Environment
resource "tls_private_key" "ssh_key" {
  count     = var.env_count
  algorithm = "RSA"
  rsa_bits  = 2048
}

resource "aws_key_pair" "generated" {
  count      = var.env_count
  key_name   = "workshop-key-env${count.index + 1}"
  public_key = tls_private_key.ssh_key[count.index].public_key_openssh
}

# 2. Security Group (Isolation Logic)
resource "aws_security_group" "env_sg" {
  count       = var.env_count
  name        = "workshop-sg-env${count.index + 1}"
  description = "Isolation for Env ${count.index + 1}"
  vpc_id      = aws_vpc.workshop_vpc.id

  # Allow all traffic from WITHIN this specific security group (Self)
  ingress {
    from_port = 0
    to_port   = 0
    protocol  = "-1"
    self      = true
  }

  # Allow SSH from world (or your IP)
  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = [var.my_ip]
  }

  # Allow Standard Web/IPA ports from world (80, 443)
  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  # Egress to everywhere (required for yum, ntp, etc)
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

# 3. Instance: IPA Server
resource "aws_instance" "ipa" {
  count                  = var.env_count
  ami                    = var.workshop_ami_id
  instance_type          = "t3.medium"
  subnet_id              = aws_subnet.main.id
  key_name               = aws_key_pair.generated[count.index].key_name
  vpc_security_group_ids = [aws_security_group.env_sg[count.index].id]

  # Cloud-Init to set hostname
  user_data = <<-EOF
    #!/bin/bash
    hostnamectl set-hostname ipa.e${count.index + 1}.${var.base_domain}
  EOF

  tags = { Name = "env${count.index + 1}-ipa" }
}

# 4. Instance: Client
resource "aws_instance" "client" {
  count                  = var.env_count
  ami                    = var.workshop_ami_id
  instance_type          = "t3.micro"
  subnet_id              = aws_subnet.main.id
  key_name               = aws_key_pair.generated[count.index].key_name
  vpc_security_group_ids = [aws_security_group.env_sg[count.index].id]

  user_data = <<-EOF
    #!/bin/bash
    hostnamectl set-hostname client.e${count.index + 1}.${var.base_domain}
  EOF

  tags = { Name = "env${count.index + 1}-client" }
}

# 5. Instance: Web
resource "aws_instance" "web" {
  count                  = var.env_count
  ami                    = var.workshop_ami_id
  instance_type          = "t3.micro"
  subnet_id              = aws_subnet.main.id
  key_name               = aws_key_pair.generated[count.index].key_name
  vpc_security_group_ids = [aws_security_group.env_sg[count.index].id]

  user_data = <<-EOF
    #!/bin/bash
    hostnamectl set-hostname web.e${count.index + 1}.${var.base_domain}
  EOF

  tags = { Name = "env${count.index + 1}-web" }
}

# --- DNS RECORDS ---

resource "aws_route53_record" "ipa" {
  count   = var.env_count
  zone_id = var.route53_zone_id
  name    = "ipa.e${count.index + 1}.${var.base_domain}"
  type    = "A"
  ttl     = "60"
  records = [aws_instance.ipa[count.index].public_ip]
}

resource "aws_route53_record" "client" {
  count   = var.env_count
  zone_id = var.route53_zone_id
  name    = "client.e${count.index + 1}.${var.base_domain}"
  type    = "A"
  ttl     = "60"
  records = [aws_instance.client[count.index].public_ip]
}

resource "aws_route53_record" "web" {
  count   = var.env_count
  zone_id = var.route53_zone_id
  name    = "web.e${count.index + 1}.${var.base_domain}"
  type    = "A"
  ttl     = "60"
  records = [aws_instance.web[count.index].public_ip]
}
