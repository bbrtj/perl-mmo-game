package CLI::Command::export_mo;

use My::Moose;
use Path::Tiny qw(cwd);
use autodie;

use header;

BEGIN { extends 'CLI::Command' }

use constant description => 'generate .mo files from translations';
use constant usage => __PACKAGE__->extract_usage;

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
		print {$fh} SimplePO->new(filename => $translation->stringify)->export;
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
		isa => Str,
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

