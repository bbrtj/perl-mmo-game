package Game::Common;

use header;
use Game::Common::Container qw(set_container);
use Mojo::Pg;
use Mojo::File qw(path);

our $VERSION = "0.001";

sub get_config ($class, $app, $file)
{

	# Load configuration from config file
	my $config = $app->plugin('Config', file => $file);
	my $config_local = $app->plugin('Config', file => "$file.local");

	return {%$config, %$config_local};
}

sub load_classes ($class, $namespace, $pattern)
{
	if ($pattern !~ m{^/}) {
		$pattern = path((caller)[1])->dirname->to_string . "/$pattern";
	}

	my @classes = map { m{/(\w+)\.pm$}; "${namespace}::$1" } glob $pattern;
	for my $class (@classes) {
		eval "use $class";
		die "error loading $class" if $@;
	}

	return @classes;
}

1;
