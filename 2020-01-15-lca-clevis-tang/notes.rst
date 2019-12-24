https://github.com/latchset/nagios-tang


::

  commit 81eca61caafadf06f71e5936d6750583d8e3a074
  Author: Tibor Dudlák <tdudlak@redhat.com>
  Date:   Sun Jul 29 18:15:20 2018 +0200

      Let world know how lightweight Tang really is
      
      Add an example configuration file for xinetd daemon.
      Alter README.md to highlight that Tang can be run on
      small devices such as Wi-Fi routers running Openwrt
      using xinetd for socket activation.
      
      Signed-off-by: Tibor Dudlák <tdudlak@redhat.com>



Demo
====

On server::

  [root@rhel82-1 tang]# systemctl enable --now tangd.socket
  Created symlink /etc/systemd/system/multi-user.target.wants/tangd.socket → /usr/lib/systemd/system/tangd.socket.

  [root@rhel82-1 tang]# find /var/db/tang
  /var/db/tang


Get advertisement::

  [root@rhel82-1 tang]# curl --silent http://localhost/adv
  {"payload":"eyJrZXlzIjpbeyJhbGciOiJFQ01SIiwiY3J2IjoiUC01MjEiLCJrZXlfb3BzIjpbImRlcml2ZUtleSJdLCJrdHkiOiJFQyIsIngiOiJBVU5VZjFLeWo2YldwTUJ0SURBZ2NfLWZzdDBtR1NRUzhnQmZ4OTJ2ZUNGSnBPRE5BLVR6YWQxLUd6c0oxb0tLMkcwNUpjdTE1YWtJeUhYZGl3WkNmbnZvIiwieSI6IkFXODhUT0wzUDhYX2hKYXZVd01Wb0llelgxUWFraGotbElTY0VvS3E4WWlZQWVyWXhjQXhiQ1lKMjhFRFMyVEMxbmx0TWVQSHZmNnBTMDl0X0hHRlFBR3gifSx7ImFsZyI6IkVTNTEyIiwiY3J2IjoiUC01MjEiLCJrZXlfb3BzIjpbInZlcmlmeSJdLCJrdHkiOiJFQyIsIngiOiJBUUpRd1VJOVJZWEJCREJYdjVrVU1WNHdQamd5UGMyeHo2U3JsWWxNUk9sOFBUMU1KaDkyMTVweW14cXRzcm83aHM1RGlQWDdGOVJsTU5ZcEtNZXBjRUxqIiwieSI6IkFFR0NDUGFrNFVMRWhxYTJrbU9NUWRsNVdxY1E2TnBJOWdhV0tycVlnOU1XcGFxT3c5MmNXWDlvX3JuOFk2Y25Pdmdac0xDclN4aTZaeXFSX2NhWjVseHAifV19","protected":"eyJhbGciOiJFUzUxMiIsImN0eSI6Imp3ay1zZXQranNvbiJ9","signature":"ANyP-WTPhaQRjgISM0W1Cc5GAoC6Iuq79tnquHJTVOf4-PZE60gqq-aERGPU-L3AzBIr6QJuDHHXOonfd4sm-mF8ADEb2hiM7RNXc1Dm2YOQD5A5eA8bi7kvqDaV7XFwyMyXBZfnrWDGr9VI2chwyvb0rwiYsB7X4Cc8Tj6UgRztAcP8"}

  [root@rhel82-0 ~]# curl --silent http://localhost/adv \
      | jq .payload | sed 's/"//g' | base64 -d
  {"keys":[{"alg":"ECMR","crv":"P-521","ke...


Inspect key database::

  [root@rhel82-1 tang]# find /var/db/tang
  /var/db/tang
  /var/db/tang/uHzDeUYdmbWhkKaosal78iUneSo.jwk
  /var/db/tang/4c5IIRUbbPhjIeXRR0soEhy6elU.jwk


tpm2 pin
--------

::

  [root@rhel82-0 ~]# echo "hello world" | clevis-encrypt-tpm2 '{}' > tpm2.jwe

  [root@rhel82-0 ~]# clevis-decrypt < tpm2.jwe 
  hello world


LUKS
----

::

  [ftweedal@f31-fde ~]$ sudo clevis luks bind -d /dev/vda2 sss "$(cat tang-cfg.json)"
  The advertisement contains the following signing keys:

  oa1fjPkDrZFxiKU_iNqLYQF9zX4

  Do you wish to trust these keys? [ynYN] y
  The advertisement contains the following signing keys:

  _vZClTtZK7HVUvkUNu065boU85w

  Do you wish to trust these keys? [ynYN] y
  The advertisement contains the following signing keys:

  sFEBJ36Ydh6eLk4OM_82OCtzcUA

  Do you wish to trust these keys? [ynYN] y
  Enter existing LUKS password: 


  [ftweedal@f31-fde ~]$ dracut -fv --regenerate-all
  ...


Polcies
-------

2 of 3 tang servers::

  {"t":2,
   "pins":{"tang":[
    {"url":"http://rhel82-0.ipa.local"},
    {"url":"http://rhel82-0.ipa.local"},
    {"url":"http://rhel82-0.ipa.local"}]
   }
  }

2 of 3 tang servers, plus TPM::

  {"t":2,
   "pins":{"tpm2":[{}],
           "sss":[
                  {"t":2,
                   "pins":{"tang":[
                    {"url":"http://rhel82-0.ipa.local"},
                    {"url":"http://rhel82-0.ipa.local"},
                    {"url":"http://rhel82-0.ipa.local"}]
                   }
                  }
                 ]
          }
  }
