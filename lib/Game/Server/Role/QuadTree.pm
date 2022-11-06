package Game::Server::Role::QuadTree;

use My::Moose::Role;
use Game::Config;

use header;

requires qw(
	location_data
);

has field '_quad_tree' => (
	isa => Types::InstanceOf ['Algorithm::QuadTree'],
	builder => 1,
	'handles->' => {
		'find_in_radius' => 'getEnclosedObjects',
	},
);

sub _build_quad_tree ($self)
{
	my $location = $self->location_data->location;
	croak 'no map for location ' . $location->id
		unless $location->data->has_map;

	return Algorithm::QuadTree->new(
		-depth => Game::Config->config->{quadtree_depth},
		-xmin => 0,
		-ymin => 0,
		-xmax => $location->data->map->size_x,
		-ymax => $location->data->map->size_y,
	);
}

sub _reload_coordinates ($self, $elapsed_time)
{
	my $qt = $self->_quad_tree;
	$qt->clear;

	state $radius = Game::Config->config->{base_radius};
	for my $actor ($self->location_data->actors->@*) {
		my $variables = $actor->variables;
		$qt->add($actor, $variables->pos_x, $variables->pos_y, $radius);
	}

	return;
}

after BUILD => sub ($self, @) {
	$self->_add_action(1 => '_reload_coordinates');
};

