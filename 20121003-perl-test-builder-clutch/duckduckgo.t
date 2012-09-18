use Modern::Perl;

use Test::More;

use DuckDuckGo;

plan tests => 1;

DuckDuckGo::search_ok('perl');
