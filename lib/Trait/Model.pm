package Trait::Model;

use v5.36;
use My::Moose::Role;

use header;

with qw(
	My::Moose::Trait::AutoSetters
	My::Moose::Trait::Serializable
);

sub add_columns_to_resultset ($self, $resultset)
{
	my @attributes = $self->serialized_attributes->@*;
	$resultset->add_columns(map { $_->name } @attributes);

	my @bool_attrs = $self->boolean_attrs;
	if (@bool_attrs) {
		$resultset->load_components(qw(FilterColumn));
		foreach my $attr (@bool_attrs) {
			$resultset->filter_column(
				$attr->name => {
					filter_to_storage => sub { $_[1] ? 1 : 0 },
				}
			);
		}
	}
}

sub boolean_attrs ($self)
{
	return grep { $_->type_constraint eq Types::Bool } $self->serialized_attributes->@*;
}

