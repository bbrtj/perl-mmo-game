package CLI::export_mo;

use My::Moose -constr;
use Mojo::File qw(path);

use header;

extends 'Mojolicious::Command';

use constant description => 'generate .mo files from translations';
sub usage ($self) { return $self->extract_usage }

sub run ($self, $language = undef)
{
	# Workaround for current bugs
	require Text::PO;
	require Text::PO::MO;
	@Text::PO::META = qw(Language Content-Type);

	die 'need a language' unless defined $language;

	my $translation = Text::PO->new;
	$translation->parse(path->child('i18n')->child("$language.po"));

	my $output = path->child('client')->child('data')->child('translations.mo');
	my $mo = Text::PO::MO->new($output, {
		auto_decode => 1,
		encoding => 'utf-8',
		default_encoding => 'utf-8',
	});

	$mo->write($translation, { file => $output });

	say "done, generated in $output";

	return;
}

__END__

=head1 SYNOPSIS

	Usage: APPLICATION export-mo [LANGUAGE]

