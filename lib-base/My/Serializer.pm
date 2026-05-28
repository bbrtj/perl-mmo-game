package My::Serializer;

use v5.42;

use Exporter qw(import);
use JSON::MaybeXS;

our @EXPORT = qw(__serialize __deserialize);

my $serializer = JSON::MaybeXS->new(
	convert_blessed => true,
);

sub __serialize { $serializer->encode(@_) }
sub __deserialize { $serializer->decode(@_) }

