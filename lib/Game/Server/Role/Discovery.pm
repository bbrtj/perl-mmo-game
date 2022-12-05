package Game::Server::Role::Discovery;

use My::Moose::Role;
use Game::Config;
use Resource::Discovery;

use header;

requires qw(
	location
	find_in_radius
	send_to_player
);

# TODO: clear this cache after actor leaves location;

has cached '_discovered_actors' => (
	isa => Types::HashRef [Types::HashRef [Types::InstanceOf ['Unit::Actor']]],
	default => sub { {} },
);

sub _discover_actors ($self, $actor, $found_objects, $resource)
{
	my $actor_id = $actor->id;
	my %found_prev = %{$self->_discovered_actors->{$actor_id} // {}};
	my %not_found = %found_prev;

	my @new;
	my @old;

	my $location = $self->location;
	foreach my $found_id ($found_objects->@*) {
		next if $found_id eq $actor_id || !(my $found = $location->get_actor($found_id));

		if ($found_prev{$found_id}) {
			delete $not_found{$found_id};
		}
		else {
			$found_prev{$found_id} = $found;
			push @new, $found_id;
		}
	}

	foreach my $not_found_id (keys %not_found) {
		push @old, $not_found_id;
		delete $found_prev{$not_found_id};
	}

	if (@new || @old) {
		$resource->new_actors(@new) if @new;
		$resource->old_actors(@old) if @old;
		$self->_discovered_actors->{$actor_id} = \%found_prev;
		return !!1;
	}

	return !!0;
}

sub _discover ($self)
{
	state $radius = Game::Config->config->{discover_radius};

	foreach my $actor ($self->location->get_players) {

		my $resource = Resource::Discovery->new({});
		my $variables = $actor->variables;
		my $should_send = 0;

		my $found_objects = $self->find_in_radius($variables->pos_x, $variables->pos_y, $radius);

		for my $method (qw(_discover_actors)) {
			$should_send = $self->$method($actor, $found_objects, $resource) || $should_send;
		}

		$self->send_to_player($actor->id, $resource) if $should_send;
	}

	return;
}

after BUILD => sub ($self, @) {
	$self->_add_action(4 => '_discover');
};

