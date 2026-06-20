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
	isa => HashRef [HashRef [InstanceOf ['Unit::Actor']]],
	default => sub { {} },
);

has cached '_discovered_by' => (
	writer => 1,
	isa => HashRef [ArrayRef [ULID]],
	default => sub { {} },
);

sub get_discovered_by ($self, $key)
{
	my $discovered_by = $self->_discovered_by->{$key};
	return $discovered_by ? $discovered_by->@* : ();
}

sub is_discovered ($self, $key)
{
	return !!$self->_discovered_by->{$key};
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

	foreach my $found ($found_objects->@*) {
		my $found_id = $found->id;

		push $discovered_by->{$found_id}->@*, $actor_id;

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
		push @old, $not_found{$not_found_id};
		delete $found_prev{$not_found_id};
	}

	if (@new || @old) {
		$resource->new_actors(\@new) if @new;
		$resource->old_actors(\@old) if @old;

		$self->_discovered_actors->{$actor_id} = \%found_prev;
		return true;
	}

	return false;
}

sub _discover ($self)
{
	state $radius = Game::Config->discover_radius;
	$self->_set_discovered_by({});
	my $location = $self->location;

	foreach my $actor ($location->get_players->@*) {

		my $resource = Resource::Discovery->new;
		my $should_send = false;

		my $found_objects = $self->find_in_radius($actor->variables->xy, $radius);
		my %aspects = (
			_discover_actors => [],
		);

		foreach my $found_id ($found_objects->@*) {
			my $found;
			if (($found = $location->get_actor($found_id)) && $found != $actor) {
				push $aspects{_discover_actors}->@*, $found;
			}
		}

		for my ($method, $objects) (%aspects) {
			$should_send = $self->$method($actor, $objects, $resource) || $should_send;
		}

		$self->send_to_player($actor->id, $resource) if $should_send;
	}

	$self->resolve_queue;

	return;
}

sub actors_info ($self, $actor_id, $wanted_actors)
{
	my @wanted_actors_data;
	my $all_actors = $self->location->actors;
	foreach my $actor_id ($wanted_actors->@*) {
		push @wanted_actors_data, $all_actors->{$actor_id};
	}

	return \@wanted_actors_data;
}

after BUILD => sub ($self, @) {
	$self->_add_action(1 => '_discover');
};

after signal_player_left => sub ($self, $actor) {
	delete $self->_discovered_actors->{$actor->id};
};

