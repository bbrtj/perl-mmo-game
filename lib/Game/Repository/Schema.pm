package Game::Repository::Schema;

use Moo;
use DI;
use Types;
use Game::Exception::RecordDoesNotExist;

use header;

with 'Game::Repository::Role::Resource';

sub save ($self, $model, $update = 0)
{
	state $type_check = Types::ConsumerOf ['Game::Model'];
	$type_check->assert_valid($model);

	my $class = $model->get_result_class;
	my $type = $update ? 'update' : 'create';
	# TODO: mark fields for update
	return DI->get('db')->dbc->resultset($class)->$type($model->serialize);
}

sub load ($self, $resultset, $search)
{
	$search = {id => $search}
		unless ref $search;

	my $found = DI->get('db')->dbc->resultset($resultset)->single($search);
	Game::Exception::RecordDoesNotExist->throw unless $found;

	return $found->to_model;
}

