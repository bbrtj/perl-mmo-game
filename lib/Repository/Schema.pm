package Repository::Schema;

use My::Moose;
use Types;
use Schema::Utils qw(ensure_single);

use header;

### Schema is the most basic way to save / load models

with 'Repository::Role::Resource';

has 'db' => (
	is => 'ro',
);

sub save ($self, $model, $update = 0)
{
	state $type_check = Types::ConsumerOf ['Model::Role::Stored'];
	$type_check->assert_valid($model);

	my $class = $model->get_result_class;
	my $type = $update ? 'update' : 'insert';
	my $dbmodel = $self->db->dbc->resultset($class)->new($model->serialize);
	if ($update) {
		my %dirty = $model->_dirty->%*;
		return unless %dirty;

		$dbmodel->in_storage(1);
		$dbmodel->make_column_dirty($_) for keys %dirty;
	}
	$dbmodel->$type;
	$model->_clear_dirty;
	return;
}

sub update
{
	$_[2] = 1;    # update flag
	goto \&save;
}

sub load ($self, $resultset, $search)
{
	$search = {id => $search}
		unless ref $search;

	my $rs = $self->db->dbc->resultset($resultset)->search($search);

	return ensure_single($rs)->to_model;
}

