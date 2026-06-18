use experimental 'class';

class Resource::X :isa(Resource);

use header;

use constant type => 'error';

field $exception :param(subject);    # X::Pub

method generate ()
{
	return $exception;
}

