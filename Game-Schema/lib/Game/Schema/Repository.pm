package Game::Schema::Repository;

use header;
use Moo;
use Game::Types qw(ConsumerOf);

no header;

sub save ($self, $model)
{
	state $type_check = ConsumerOf ['Game::Model'];
}
1;
