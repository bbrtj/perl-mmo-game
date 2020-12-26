use v5.30;
use File::Basename;

my $current = dirname(__FILE__);
my $current_uri = 'file://' . $current;
my @modules = qw(
	Game::Common
	Game::Schema
	Game::Repository
	Game::Unit
	Game::Worker
	Game::Lore
);

# cpanfile mostly requests modules, real dependencies are in makefiles
for my $module (@modules) {
	my $filename = $module =~ s/::/-/gr;
	my @dists = sort grep defined, map { /$filename-(.+)\.tar\.gz$/; $1 }
		glob "${current}/${filename}/${filename}-*.tar.gz";

	die "No dist for $module was found" unless @dists > 0;

	my $version = pop @dists;
	requires $module, $version,
		url => "${current_uri}/${filename}/${filename}-${version}.tar.gz";
}
