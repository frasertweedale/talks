use Modern::Perl;

use Test::More;
use Test::Builder::Clutch;

plan tests => 1;

sub hello_ok { say "Hello, world!"; pass; }

BEGIN { Test::Builder::Clutch::antitest 'hello_ok' }

hello_ok;
hello;
