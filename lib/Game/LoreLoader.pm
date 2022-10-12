package Game::LoreLoader;

use Mojo::File qw(curfile path);
use Encode qw(decode);

use header;

use constant DIRECTORY => curfile->dirname->dirname->dirname->child('game-data');
use constant EXTENSION => 'gd';

my %loaded;

sub load ($self, $name)
{
	$name =~ s/\.@{[EXTENSION]}$//;
	return if $loaded{$name};

	my $filename = DIRECTORY->child("$name." . EXTENSION);
	die "$filename does not exist"
		unless -e $filename;

	my $contents = decode 'utf-8', $filename->slurp;

	state $last_id = 1;
	my $package = "Game::LoreLoader::Sandbox" . $last_id++;

	$contents = <<~PERL;
		package $package;
		use v5.36;
		use utf8;

		use constant FILENAME => '$filename';

		use Game::LoreLoader::DSL;

		$contents
		PERL

	my $error = do {
		local $@ = undef;
		my $success = eval "$contents; 1";    ## no critic 'BuiltinFunctions::ProhibitStringyEval'
		$success ? undef : $@;
	};

	die $error if defined $error;
	$loaded{$name} = 1;
	return;
}

sub load_all ($self)
{
	for my $file (DIRECTORY->list_tree->each) {
		next unless $file->extname eq EXTENSION;
		$self->load($file->to_rel(DIRECTORY));
	}

	return;
}

