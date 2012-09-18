use Test::More;
use Test::Builder::Clutch;

plan tests => 2;

pass;
Test::Builder::Clutch::disengage;
fail;
Test::Builder::Clutch::engage;
pass;
