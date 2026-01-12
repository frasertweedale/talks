# AWS IAM policies for the PKI workshop

To avoid dependencies on external SSO and minimise hiccups relating
to session token expiry, you can create an IAM user account for
creating and destroying the workshop infrastructure.

The account should be granted permissions for the required actions
by attaching *Policies*, as described below.


## EC2

Because many EC2 operations are required, it is reasonable to use
the AWS managed `AmazonEC2FullAccess` policy.


## Route53

`route53.json` contains a recommended Route53 policy which allows
management of DNS records in the specified *Hosted Zone*.  The ID in
the hosted zone ARN must be changed to the relevant value.

The workshop also requires a *Private* Hosted Zone for configuring
"split horizon DNS".  The Terraform spec sets the "Owner" tag of the
created Hosted Zone to the user ID of the account that creates it.
The IAM policy uses policy variables to ensure that only *owned*
hosted zones can be managed, in addition to the nominated public
hosted zone.


## Other services

The `sts:GetCallerIdentity` permission is required.  `sts.json` is
an example policy.  Your account or organisation may already have a
suitable policy object.
