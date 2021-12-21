package Repository::Schema;

use Moo;
use DI;
use Types;
use Game::Exception::RecordDoesNotExist;

use header;

with 'Repository::Role::Resource';

has 'db' => (
	is => 'ro',
);

sub save ($self, $model, $update = 0)
{
	state $type_check = Types::ConsumerOf ['Game::Model::Role::Stored'];
	$type_check->assert_valid($model);

	my $class = $model->get_result_class;
	my $type = $update ? 'update' : 'insert';
	my $dbmodel = $self->db->dbc->resultset($class)->new($model->serialize);
	if ($update) {
		$dbmodel->in_storage(1);
		$dbmodel->make_column_dirty($_) for keys $model->_dirty->%*;
	}
	$dbmodel->$type;
	return;
}

sub load ($self, $resultset, $search)
{
	$search = {id => $search}
		unless ref $search;

	my $found = $self->db->dbc->resultset($resultset)->single($search);
	Game::Exception::RecordDoesNotExist->throw unless $found;

	return $found->to_model;
}

