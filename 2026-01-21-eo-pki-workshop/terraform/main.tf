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

  # Cloud-Init to set hostname and install IPA server
  user_data = <<-EOF
    #!/bin/bash
    DOMAIN="e${count.index + 1}.${var.base_domain}"
    REALM=$(echo $DOMAIN | tr '[:lower:]' '[:upper:]')

    hostnamectl set-hostname ipa.$DOMAIN

    ipa-server-install -U \
      --no-ntp \
      --realm=$REALM \
      --ds-password=Secret.123 \
      --admin-password=Secret.123 \
      --mkhomedir
    echo Secret.123 | kinit admin
    ipa user-add user1 --first User --last One
    echo Secret.123 | ipa passwd user1
    ipa user-add user2 --first User --last Two
    echo Secret.123 | ipa passwd user2
    ipa group-add sclogin
    ipa group-add-member sclogin --users user2
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
    DOMAIN="e${count.index + 1}.${var.base_domain}"
    hostnamectl set-hostname client.$DOMAIN

    # Wait for IPA server to come up
    # We verify the HTTP code is < 400 (server is responding)
    echo "Waiting for IPA Server to be ready..."
    until curl -s -k --output /dev/null --fail https://ipa.$DOMAIN/ipa/ui/; do
      sleep 30
    done
    echo "IPA Server is up. Installation will proceed in 2 minutes."
    sleep 120

    ipa-client-install -U \
      --no-ntp \
      --server=ipa.$DOMAIN \
      --domain=$DOMAIN \
      --principal=admin \
      --password="Secret.123" \
      --force-join \
      --mkhomedir
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

# --- SPLIT-HORIZON DNS CONFIGURATION ---

# 1. Create the Private Hosted Zone
# This zone is only visible to machines inside the VPC.
resource "aws_route53_zone" "private" {
  name = var.base_domain

  vpc {
    vpc_id = aws_vpc.workshop_vpc.id
  }

  tags = {
    Name = "PKI-Workshop-Private-Zone"
  }
}

# 2. INTERNAL Records (Point to PRIVATE IPs in PRIVATE Zone)
# These enable the "self=true" Security Group rules to work.

resource "aws_route53_record" "ipa_private" {
  count   = var.env_count
  zone_id = aws_route53_zone.private.zone_id
  name    = "ipa.e${count.index + 1}.${var.base_domain}"
  type    = "A"
  ttl     = "60"
  records = [aws_instance.ipa[count.index].private_ip]
}

resource "aws_route53_record" "ipa_ca_private" {
  count   = var.env_count
  zone_id = aws_route53_zone.private.zone_id
  name    = "ipa-ca.e${count.index + 1}.${var.base_domain}"
  type    = "A"
  ttl     = "60"
  records = [aws_instance.ipa[count.index].private_ip]
}

resource "aws_route53_record" "client_private" {
  count   = var.env_count
  zone_id = aws_route53_zone.private.zone_id
  name    = "client.e${count.index + 1}.${var.base_domain}"
  type    = "A"
  ttl     = "60"
  records = [aws_instance.client[count.index].private_ip]
}

resource "aws_route53_record" "web_private" {
  count   = var.env_count
  zone_id = aws_route53_zone.private.zone_id
  name    = "web.e${count.index + 1}.${var.base_domain}"
  type    = "A"
  ttl     = "60"
  records = [aws_instance.web[count.index].private_ip]
}

# 3. PUBLIC Records (Point to PUBLIC IPs in PUBLIC Zone)
# These allow students to SSH in and access the web UI from the internet.

resource "aws_route53_record" "ipa_public" {
  count   = var.env_count
  zone_id = var.public_zone_id  # Note: Using the variable for the external zone
  name    = "ipa.e${count.index + 1}.${var.base_domain}"
  type    = "A"
  ttl     = "60"
  records = [aws_instance.ipa[count.index].public_ip]
}

resource "aws_route53_record" "ipa_ca_public" {
  count   = var.env_count
  zone_id = var.public_zone_id  # Note: Using the variable for the external zone
  name    = "ipa-ca.e${count.index + 1}.${var.base_domain}"
  type    = "A"
  ttl     = "60"
  records = [aws_instance.ipa[count.index].public_ip]
}

resource "aws_route53_record" "client_public" {
  count   = var.env_count
  zone_id = var.public_zone_id
  name    = "client.e${count.index + 1}.${var.base_domain}"
  type    = "A"
  ttl     = "60"
  records = [aws_instance.client[count.index].public_ip]
}

resource "aws_route53_record" "web_public" {
  count   = var.env_count
  zone_id = var.public_zone_id
  name    = "web.e${count.index + 1}.${var.base_domain}"
  type    = "A"
  ttl     = "60"
  records = [aws_instance.web[count.index].public_ip]
}
