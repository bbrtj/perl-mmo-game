package Repository::Models;

use My::Moose;
use Schema::Utils qw(fetch_single);

use header;

### Schema is the most basic way to save / load models

extends 'Repository';

has injected 'db';

sub save ($self, $model, $update = 0)
{
	state $type_check = Types::ConsumerOf ['Model::Role::Stored'];
	$type_check->assert_valid($model);

	my @dirty = $model->_dirty;
	return if $update && @dirty == 0;

	my $type = $update ? 'update' : 'insert';
	my $dbmodel = $self->db->dbc
		->resultset($model->get_result_class)
		->new($model->serialize);

	if ($update) {
		$dbmodel->in_storage(1);
		$dbmodel->make_column_dirty($_) for @dirty;
	}

	$dbmodel->$type;
	$model->_clear_dirty unless @dirty == 0;
	return;
}

sub update ($self, $model)
{
	return $self->save($model, 1);
}

sub load ($self, $resultset, $search)
{
	$search = {id => $search}
		unless ref $search;

	my $rs = $self->db->dbc->resultset($resultset)->search($search);

	return fetch_single($rs)->to_model;
}

