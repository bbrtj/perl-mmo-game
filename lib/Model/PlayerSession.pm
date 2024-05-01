package Model::PlayerSession;

use My::Moose;

use header;

use enum qw(:STATE_ NEW LOGGED_IN PLAYING);
use constant SESSION_STATES => [STATE_NEW .. STATE_PLAYING];

extends 'Model';

with 'Model::Role::Identified';

has option 'user_id' => (
	isa => Types::ULID,
	clearer => 1,
);

has param 'state' => (
	isa => Types::Enum [@{+SESSION_STATES}],
	default => STATE_NEW,
);

has option 'player_id' => (
	isa => Types::ULID,
	clearer => 1,
);

# in-game location
has option 'location_id' => (
	isa => Types::LoreId,
);

sub set_logged_in ($self, $user_id)
{
	$self->set_user_id($user_id);
	$self->set_state(STATE_LOGGED_IN);

	return;
}

sub set_playing ($self, $actor)
{
	$self->set_player_id($actor->player->id);
	$self->set_location_id($actor->variables->location_id);
	$self->set_state(STATE_PLAYING);

	return;
}

sub is_logged_in ($self)
{
	my $state = $self->state;
	return any { $state == $_ } STATE_LOGGED_IN, STATE_PLAYING;
}

sub is_playing ($self)
{
	return $self->state == STATE_PLAYING;
}

__PACKAGE__->_register_cache;

