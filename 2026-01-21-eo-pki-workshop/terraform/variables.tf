variable "region" {
  default = "ap-southeast-2"
}

variable "workshop_ami_id" {
  description = "The ID of your Golden AMI (Fedora + Packages)"
  type        = string
  default     = "ami-076c9905b60d22ae0" # v2
}

variable "base_domain" {
  default = "pki.frase.id.au"
}

variable "route53_zone_id" {
  description = "The Hosted Zone ID for pki.frase.id.au (Found in Route53 console)"
  type        = string
	default     = "Z07020222557K96HMZHF8"
}

variable "env_count" {
  description = "How many workshops to spawn"
  default     = 2
}

variable "my_ip" {
  description = "Your IP address for SSH access restrictions (Optional, 0.0.0.0/0 allows all)"
  default     = "0.0.0.0/0"
}
