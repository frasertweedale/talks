use Modern::Perl;

use List::Enumerator qw( E );

use DuckDuckGo;

my $term = join ' ', @ARGV;
say "searching for '$term'";
E(@{DuckDuckGo::search($term)})->with_index->each(
	sub {
		my ($url, $index) = @$_;
		say "$index $url";
	}
);
