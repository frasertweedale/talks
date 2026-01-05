resource "local_file" "ssh_keys" {
  count           = var.env_count
  content         = tls_private_key.ssh_key[count.index].private_key_openssh
  filename        = "${path.module}/keys/env${count.index + 1}.pem"
  file_permission = "0600"
}

output "connection_info" {
  value = [for i in range(var.env_count) : {
    environment = "env${i + 1}"
    ipa_dns     = "ipa.e${i + 1}.${var.base_domain}"
    ssh_command = "ssh -i keys/env${i + 1}.pem fedora@ipa.e${i + 1}.${var.base_domain}"
  }]
}
