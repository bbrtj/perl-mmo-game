package CLI::export_mo;

use My::Moose -constr;
use Mojo::File qw(path);

use header;

extends 'Mojolicious::Command';

use constant description => 'generate .mo files from translations';
sub usage ($self) { return $self->extract_usage }

sub run ($self, $language = undef)
{
	die 'need a language' unless defined $language;

	my $translation = path->child('i18n')->child("$language.po");
	my $output = path->child('client')->child('data')->child('translations.mo');
	`msgfmt $translation -o $output`;

	say "done, generated in $output";

	return;
}

__END__

=head1 SYNOPSIS

	Usage: APPLICATION export-mo [LANGUAGE]

