package My::Serializer;

use v5.36;

use Exporter qw(import);
use Mojo::JSON qw(to_json from_json);

our @EXPORT = qw(__serialize __deserialize);

{
	no strict 'refs';
	*{__PACKAGE__ . '::__serialize'} = \&to_json;
	*{__PACKAGE__ . '::__deserialize'} = \&from_json;
}

1;

