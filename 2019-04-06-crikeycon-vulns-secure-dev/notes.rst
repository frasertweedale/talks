- Dogtag authz CVE (boolean blindness)
  CVE-2018-1080 (moderate impact)
    https://access.redhat.com/security/cve/cve-2018-1080
  https://bugzilla.redhat.com/show_bug.cgi?id=CVE-2018-1080
  b54975f4cac60e2f4332b08414f1b5ea4de62601
  Fixed in Dogtag PKI 10.6.1
  https://frasertweedale.github.io/blog-redhat/posts/2018-03-26-old-dogtag-new-tricks.html

- FreeIPA RA Agent authz CVE (priv sep)
  CVE-2016-5404
    https://access.redhat.com/security/cve/cve-2016-5404
    fixed in freeipa-4.3.3, freeipa-4.4.1 and later
    cf74584d0f772f3f5eccc1d30c001e4212a104fd
    https://pagure.io/freeipa/issue/6232
    can revoke any certificate (DOS vuln)
  CVE-2017-2590
    fixed in freeipa-4.5.0
    b81ac59640f0b76fa9f53cf8be441f085a7089c4
    https://pagure.io/freeipa/issue/6713
    can enable/disable/delete CA (including keys!)


- Firefox python-crypto big OID (input assumptions, asan etc)
  python-cryptography PR:
    https://github.com/pyca/cryptography/pull/3612
  CPython PR:
    https://github.com/python/cpython/pull/2909
    https://bugs.python.org/issue30502
  OpenSSL man page:
    https://www.openssl.org/docs/manmaster/man3/OBJ_nid2ln.html#return_values
  CVE-2017-7792
  https://bugzilla.mozilla.org/show_bug.cgi?id=1368652
  Fix commit:
    https://hg.mozilla.org/mozilla-central/rev/b274e6e81c8b
  Fixed in Firefox 55
    https://www.mozilla.org/en-US/security/advisories/mfsa2017-18/
  Fixed in Thunderbird 52.3
    https://www.mozilla.org/en-US/security/advisories/mfsa2017-20/
  Try f26 or older RHEL

- login fail (exceptions are bad; negative tests)
   (no cve)
