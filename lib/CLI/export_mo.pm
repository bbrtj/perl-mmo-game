package CLI::export_mo;

use My::Moose -constr;
use Path::Tiny qw(cwd);
use autodie;

use header;

extends 'Mojolicious::Command';

use constant description => 'generate .mo files from translations';
sub usage ($self) { return $self->extract_usage }

sub run ($self, $language = undef)
{
	unless (defined $language) {
		$self->help;
		return;
	}

	my $translation = cwd->child('i18n')->child("$language.yml");
	my $output = cwd->child('client')->child('data')->child('translations.mo');

	{
		open my $fh, '|-:encoding(UTF-8)', "msgfmt - -o $output";
		print {$fh} SimplePO->new(filename => $translation->to_string)->export;
	}

	say "done, generated in $output";

	return;
}

## no critic 'Modules::ProhibitMultiplePackages'
package SimplePO {
	use My::Moose;
	use YAML::PP qw(LoadFile);

	use header;

	has param 'filename' => (
		isa => Types::Str,
	);

	sub export ($self)
	{
		my @translations = LoadFile($self->filename);
		my ($lang) = $self->filename =~ m{(?: / | ^ ) (.+) \.ya?ml$}x;

		my $content = <<~PO;
		msgid ""
		msgstr ""
		"Language: $lang\\n"
		"Content-Type: text/plain; charset=UTF-8\\n"

		PO

		for my $translation (@translations) {

			# FIXME: escape double quotes?

			$content .= <<~PO;
			msgid "$translation->{id}"
			msgstr "$translation->{str}"

			PO
		}

		return $content;
	}

}

__END__

=head1 SYNOPSIS

	Usage: APPLICATION export-mo [LANGUAGE]

