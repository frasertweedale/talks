variable "region" {
  default = "eu-central-1"
}

variable "workshop_ami_id_main" {
  type        = string
  #default     = "ami-00688f06e94d76a7b" # pki-workshop-f43-v4-tpm ap-southeast-2
  default     = "ami-0501c16d695c2b0f2" # pki-workshop-f43-v4-tpm eu-central-1
}

variable "workshop_ami_id_workstation" {
  type        = string
  #default     = "ami-0863a1a91e6cb87e3" # pki-workshop-f43-workstation-v2 ap-southeast-2
  default     = "ami-0115fbc59011444f2" # pki-workshop-f43-workstation-v2 eu-central-1
}

variable "base_domain" {
  default = "cz2026.workshop.freeipa.org"
}

variable "public_zone_id" {
  description = "The AWS Hosted Zone ID for workshop.freeipa.org"
  type        = string
	default     = "Z09692993UOYQ3AYWAS0A"
}

variable "env_count" {
  description = "How many workshops to spawn"
  default     = 2
}

variable "my_ip" {
  description = "Your IP address for SSH access restrictions (Optional, 0.0.0.0/0 allows all)"
  default     = "0.0.0.0/0"
}
