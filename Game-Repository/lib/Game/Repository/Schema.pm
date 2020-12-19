package Game::Repository::Schema;

use header;
use Moo;
use Game::Types qw(ConsumerOf);

no header;

sub _save ($self, $model, $update = 0)
{
	state $type_check = ConsumerOf ['Game::Model'];
	$type_check->assert_valid($model);

	my $class = $model->get_result_class;
	my $type = $update ? 'update' : 'save';
	return resolve('dbc')->resultset($class->source_name)->$type($model->serialize);
}

sub save ($self, $model)
{
	goto _save;
}

sub update ($self, $model)
{
	push @_, 1;
	goto _save;
}

1;
