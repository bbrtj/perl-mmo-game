package Game::Repository::Schema;

use header;
use Moo;
use Game::Common::Container;
use Game::Types qw(ConsumerOf);
use Game::Exception::RecordDoesNotExist;

no header;

with 'Game::Repository::Role::Resource';

sub save ($self, $model, $update = 0)
{
	state $type_check = ConsumerOf ['Game::Model'];
	$type_check->assert_valid($model);

	my $class = $model->get_result_class;
	my $type = $update ? 'update' : 'create';
	return resolve('dbc')->resultset($class)->$type($model->serialize);
}

sub load ($self, $resultset, $search)
{
	$search = {id => $search}
		unless ref $search;

	my $found = resolve('dbc')->resultset($resultset)->single($search);
	Game::Exception::RecordDoesNotExist->throw unless $found;

	return $found->to_model;
}

1;
