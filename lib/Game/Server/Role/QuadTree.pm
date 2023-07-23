package Game::Server::Role::QuadTree;

use My::Moose::Role;
use Game::Config;
use POSIX qw(ceil);
use Algorithm::QuadTree;

use header;

requires qw(
	location
);

sub find_in_radius;
has field '_quad_tree' => (
	isa => Types::InstanceOf ['Algorithm::QuadTree'],
	lazy => 1,
	'handles->' => {
		'find_in_radius' => 'getEnclosedObjects',
	},
);

sub _build_quad_tree ($self)
{
	my $size = (sort { $b <=> $a } ($self->map->size_x, $self->map->size_y))[0];
	my $required_precision = $size / Game::Config->config->{base_radius};
	my $required_depth = ceil(log($required_precision) / log(2));

	return Algorithm::QuadTree->new(
		-depth => $required_depth,
		-xmin => 0,
		-ymin => 0,
		-xmax => $self->map->size_x,
		-ymax => $self->map->size_y,
	);
}

sub _reload_coordinates ($self)
{
	my $qt = $self->_quad_tree;
	$qt->clear;

	my $radius = Game::Config->config->{base_radius};
	foreach my $actor (values $self->location->actors->%*) {
		$qt->add($actor->id, $actor->variables->pos_x, $actor->variables->pos_y, $radius);
	}

	return;
}

after BUILD => sub ($self, @) {
	$self->_add_action(1 => '_reload_coordinates');
};

