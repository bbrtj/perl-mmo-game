package Model::PlayerSession;

use My::Moose;
use Game::Config;

use header;

use constant {
	STATE_DISABLED => -1,    # special state for things that are disabled
	STATE_NEW => 0,
	STATE_LOGGED_IN => 1,
	STATE_PLAYING => 2,
};

extends 'Model';

with 'Model::Role::Identified';

has option 'user_id' => (
	isa => Types::ULID,
	clearer => 1,
);

has param 'state' => (
	isa => Types::Enum [STATE_NEW, STATE_LOGGED_IN, STATE_PLAYING],
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

