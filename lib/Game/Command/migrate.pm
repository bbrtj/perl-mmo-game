package Game::Command::migrate;

use Mojo::Base 'Mojolicious::Command', -signatures;
use Getopt::Long qw(GetOptionsFromArray);
use Mojo::File qw(curfile);

has description => 'migrate database schema';

# Usage message from SYNOPSIS
has usage => sub ($self) { $self->extract_usage };

sub get_files()
{
	my $dir = curfile->dirname->dirname->sibling('migrations')->to_string;
	return glob "$dir/*.sql";
}

sub run($self, @args)
{
	my $up = 0;
	my $down = 0;
	my $downall = 0;

	GetOptionsFromArray(
		\@args,
		"up" => \$up,
		"down" => \$down,
		"downall" => \$downall,
	);


	my $migrations = $self->app->db->migrations;
	foreach my $file (get_files) {
		$migrations->from_file($file);
	}

	my $version = $migrations->active;

	if ($down) {
		$migrations->migrate($version - 1)
			if $version;
	}

	elsif ($downall) {
		$migrations->migrate(0);
	}

	elsif ($up) {
		$migrations->migrate;
	}

	$version = $migrations->active;
	my $latest = $migrations->latest;
	say "Currently at version $version / $latest";
}

1;

__END__
=head1 SYNOPSIS
	Usage: APPLICATION migrate [OPTIONS]
	Options:
		--up  migrates up
		--down  migrates down one migration
		--downall  migrates down all migrations
