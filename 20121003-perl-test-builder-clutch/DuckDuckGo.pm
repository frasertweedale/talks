package DuckDuckGo;

use Modern::Perl;

use Test::WWW::Mechanize;
use Test::More;
use Web::Scraper;

use Test::Builder::Clutch;


sub search_ok {
	my $term = shift;
	my $links;
	subtest 'perform search' => sub {
		plan tests => 3;
		my $mech = Test::WWW::Mechanize->new();
		$mech->get_ok("http://duckduckgo.com/html/");
		my $response = $mech->submit_form_ok(
			{ fields => { q => $term } },
			'submit the search'
		);
		my $scraper = scraper {
			process "//div[\@class='url']", "links[]" => "TEXT";
		};
		my $result = $scraper->scrape($mech->content);
		@$links = $result->{links} // [];
		ok @$links, 'got some links';
	};
	@$links ? $links : undef;
}


Test::Builder::Clutch::antitest 'search_ok';

1;
