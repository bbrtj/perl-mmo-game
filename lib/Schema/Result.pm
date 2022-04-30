package Schema::Result;

use Model;

use header;

use parent 'DBIx::Class::Core';

sub to_model ($self)
{
	return Model->from_result($self);
}

### Helpers for buliding resultsets
sub essentials ($self, $table)
{
	$self->table($table);
	$self->add_column('id');
	$self->set_primary_key('id');

	return;
}

sub belongs_to ($self, $name, $class, $column, @more)
{
	$self->add_column($column);
	return $self->SUPER::belongs_to($name, $class, $column, @more);
}

