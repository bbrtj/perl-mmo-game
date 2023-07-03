package My::Serializer;

use v5.38;

use Exporter qw(import);
use Mojo::JSON qw(to_json from_json);

our @EXPORT = qw(__serialize __deserialize);

sub __serialize { goto \&to_json }
sub __deserialize { goto \&from_json }

