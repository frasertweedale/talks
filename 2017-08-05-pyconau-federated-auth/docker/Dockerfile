FROM frasertweedale/keycloak-openshift:latest

USER root

COPY ca.pem /etc/pki/ca-trust/source/anchors/ca.pem

RUN update-ca-trust

USER jboss
