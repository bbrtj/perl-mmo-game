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

has cached '_discovered_actors' => (
	isa => Types::HashRef [Types::HashRef [Types::InstanceOf ['Unit::Actor']]],
	default => sub { {} },
);

has cached '_discovered_by' => (
	writer => 1,
	isa => Types::HashRef [Types::ArrayRef [Types::ULID]],
	default => sub { {} },
);

sub get_discovered_by ($self, $key)
{
	return ($self->_discovered_by->{$key} // [])->@*;
}

sub _discover_actors ($self, $actor, $found_objects, $resource)
{
	my $actor_id = $actor->id;
	my %found_prev = %{$self->_discovered_actors->{$actor_id} // {}};
	my %not_found = %found_prev;

	my @new;
	my @old;

	my $discovered_by = $self->_discovered_by;
	my $location = $self->location;

	foreach my $found_id ($found_objects->@*) {
		next if $found_id eq $actor_id || !(my $found = $location->get_actor($found_id));

		push $discovered_by->{$found_id}->@*, $actor_id
			if $found->is_player;

		if ($found_prev{$found_id}) {
			delete $not_found{$found_id};
		}
		else {
			$found_prev{$found_id} = $found;
			push @new, $found;
			$self->queue('signal_actor_appeared', $actor, $found);
		}
	}

	foreach my $not_found_id (keys %not_found) {
		push @old, $not_found_id;
		delete $found_prev{$not_found_id};
	}

	if (@new || @old) {
		$resource->new_actors(\@new) if @new;
		$resource->old_actors(\@old) if @old;

		$self->_discovered_actors->{$actor_id} = \%found_prev;
		return !!1;
	}

	return !!0;
}

sub _discover ($self)
{
	state $radius = Game::Config->config->{discover_radius};
	$self->_set_discovered_by({});

	foreach my $actor ($self->location->get_players) {

		my $resource = Resource::Discovery->new;
		my $variables = $actor->variables;
		my $should_send = 0;

		my $found_objects = $self->find_in_radius($variables->pos_x, $variables->pos_y, $radius);

		for my $method (qw(_discover_actors)) {
			$should_send = $self->$method($actor, $found_objects, $resource) || $should_send;
		}

		$self->send_to_player($actor->id, $resource) if $should_send;
	}

	$self->resolve_queue;

	return;
}

after BUILD => sub ($self, @) {
	$self->_add_action(6 => '_discover');
};

after signal_player_left => sub ($self, $actor) {
	delete $self->_discovered_actors->{$actor->id};
};

