variable "region" {
  default = "ap-southeast-2"
}

variable "workshop_ami_id_main" {
  type        = string
  default     = "ami-00688f06e94d76a7b" # pki-workshop-f43-v4-tpm
}

variable "workshop_ami_id_workstation" {
  type        = string
  default     = "ami-0863a1a91e6cb87e3" # pki-workshop-f43-workstation-v2
}

variable "base_domain" {
  default = "pki.frase.id.au"
}

variable "public_zone_id" {
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
