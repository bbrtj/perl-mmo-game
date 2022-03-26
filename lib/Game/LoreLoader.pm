package Game::LoreLoader;

use Mojo::File qw(curfile path);
use Utils;
use header;

use constant DIRECTORY => curfile->dirname->dirname->dirname->child('game-data');

my %loaded;

sub load ($self, $name)
{
	$name =~ s/\.gd$//;
	return if $loaded{$name};

	my $filename = DIRECTORY->child("$name.gd");
	die "$filename does not exist"
		unless -f $filename;

	my $contents = $filename->slurp;

	state $last_id = 1;
	my $package = "Game::LoreLoader::Sandbox" . $last_id++;

	$contents = <<~PERL;
		package $package;
		use v5.30;
		use warnings;
		use Game::LoreLoader::DSL;

		$contents
		PERL

	my $error = do {
		local $@;
		my $success = eval "$contents; 1;";
		$success ? undef : $@;
	};

	die $error if defined $error;
	$loaded{$name} = 1;
	return;
}

sub load_all ($self)
{
	Utils->bootstrap_lore;

	for my $file (DIRECTORY->list_tree->each) {
		next unless $file->extname eq 'gd';
		$self->load($file->to_rel(DIRECTORY));
	}
}

