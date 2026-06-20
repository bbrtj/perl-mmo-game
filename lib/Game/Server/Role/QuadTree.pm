package Game::Server::Role::QuadTree;

BEGIN { $ENV{ALGORITHM_QUADTREE_BACKEND} = 'Algorithm::QuadTree::XS::NoBackRefs'; }

use My::Moose::Role;
use Game::Config;
use Algorithm::QuadTree;
use List::Util qw(max);

use header;

requires qw(
	location
);

# precise tree for collision calculations
sub actors_collision;
has field '_combat_quad_tree' => (
	isa => InstanceOf ['Algorithm::QuadTree'],
	lazy => 1,
	'handles->' => {
		'actors_collision' => 'getEnclosedObjects',
	},
);

# coarse tree for checks which do not require much precision
sub find_in_radius;
has field '_discovery_quad_tree' => (
	isa => InstanceOf ['Algorithm::QuadTree'],
	lazy => 1,
	'handles->' => {
		'find_in_radius' => 'getEnclosedObjects',
	},
);

my sub calculate_required_depth ($self, $base_radius)
{
	my $size = max $self->map->size_x, $self->map->size_y;
	my $required_precision = $size / $base_radius;
	return ceil(log($required_precision) / log(2)) + 1;
}

sub _build_combat_quad_tree ($self)
{
	my $required_depth = $self->&calculate_required_depth(Game::Config->base_radius);
	$self->log->debug("Combat quad tree depth is $required_depth");

	return Algorithm::QuadTree->new(
		-depth => $required_depth,
		-xmin => 0,
		-ymin => 0,
		-xmax => $self->map->size_x,
		-ymax => $self->map->size_y,
	);
}

sub _build_discovery_quad_tree ($self)
{
	my $required_depth = $self->&calculate_required_depth(Game::Config->base_radius * 16);
	$self->log->debug("Discovery quad tree depth is $required_depth");

	return Algorithm::QuadTree->new(
		-depth => $required_depth,
		-xmin => 0,
		-ymin => 0,
		-xmax => $self->map->size_x,
		-ymax => $self->map->size_y,
	);
}

sub _reload_combat_coordinates ($self)
{
	my $qt = $self->_combat_quad_tree;
	$qt->clear;

	foreach my $actor (values $self->location->actors->%*) {
		# NOTE: is_discovered cannot be put into 'requires' because discovery
		# depends on quad tree
		next if $actor->is_npc && !$self->is_discovered($actor->id);

		$qt->add($actor, $actor->variables->xy, $actor->stats->size);
	}

	return;
}

sub _reload_discovery_coordinates ($self)
{
	my $qt = $self->_discovery_quad_tree;
	$qt->clear;

	foreach my $actor (values $self->location->actors->%*) {
		$qt->add($actor->id, $actor->variables->xy, $actor->stats->size);
	}

	return;
}

after BUILD => sub ($self, @) {
	$self->_add_action(0.1 => '_reload_combat_coordinates', 9);
	$self->_add_action(1 => '_reload_discovery_coordinates', 9);
};

