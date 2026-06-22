use experimental 'class';

class Resource::Success :isa(Resource);

use header;

use constant type => 'success';
use constant is_plaintext => true;

method generate ()
{
	return [];
}

