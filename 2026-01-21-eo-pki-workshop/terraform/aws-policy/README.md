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

The workshop refers to an existing *Public* Hosted Zone for
modifying publicly accessible DNS records to access the services.

The workshop also requires a *Private* Hosted Zone for configuring
"split horizon DNS".  As a consequence, the account needs
permissions to create hosted zones as well as update records.
Route53 does not expose resource tags to IAM for policy evaluation
so the options for access control are limited.  The example policy
is qute broad as a result.


## Other services

The `sts:GetCallerIdentity` permission is required.  `sts.json` is
an example policy.  Your account or organisation may already have a
suitable policy object.
