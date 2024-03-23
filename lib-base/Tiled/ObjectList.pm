package Tiled::ObjectList;

use My::Moose;

use header;

use constant TYPE_SPAWNS => 'spawns';

has param 'map' => (
	isa => Types::InstanceOf ['Tiled::Map'],
	weak_ref => 1,
);

has field 'objects' => (
	isa => Types::HashRef [Types::HashRef],
	default => sub { {} },
	'handles{}' => {
		'_add_object' => 'set',
		'get_objects_of_type' => 'get',
	},
);

sub add_object ($self, $type, $attributes, $properties = {})
{
	my %object = (
		name => $attributes->{name},
		id => $attributes->{type},
		x => $attributes->{x} / $self->map->tilewidth,
		y => $attributes->{y} / $self->map->tileheight,
		%{$properties},
	);

	$self->_add_object($type, \%object);
	return;
}

