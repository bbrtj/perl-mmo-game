package Game::Common;

use Mojo::File qw(path);

use header;

sub load_classes ($class, $namespace, $pattern)
{
	if ($pattern !~ m{^/}) {
		$pattern = path((caller)[1])->dirname->to_string . "/$pattern";
	}

	my @classes = map { m{/(\w+)\.pm$}; "${namespace}::$1" } glob $pattern;
	for my $class (@classes) {
		eval "use $class";
		die "error loading $class: $@" if $@;
	}

	return @classes;
}

