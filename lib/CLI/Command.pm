package CLI::Command;

use My::Moose;

use header;
use Pod::Text;
use Class::Inspector;

has field 'command' => (
	isa => Str,
	default => sub ($self) {
		return (ref $self) =~ s/^CLI::Command:://r;
	},
);

sub description { ... }
sub usage { ... }

sub extract_usage ($self)
{
	my $class = ref $self || $self;

	my $output;
	my $formatter = Pod::Text->new;
	$formatter->output_string(\$output);
	$formatter->parse_file('lib/' . Class::Inspector->filename($class));

	return $output;
}

sub help ($self, $header_only = false)
{
	say $self->command . ': ' . $self->description;
	say $self->usage unless $header_only;

	return;
}

