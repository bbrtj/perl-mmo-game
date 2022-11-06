package Service::Faker;

use My::Moose;
use all 'Model';
use Game::Helpers;
use Faker;
use Data::ULID qw(ulid);

use header;

has injected 'character_service';

has field 'faker' => (
	constructed => ['Faker', 'en-us'],
);

sub fake_user ($self, $plaintext_password = $self->faker->user_password)
{
	return Model::User->new(
		email => $self->faker->internet_email_address,
		plaintext_password => $plaintext_password,
	);
}

sub fake_player ($self, $user_id = ulid)
{
	return Model::Player->new(
		user_id => $user_id,
	);
}

sub fake_character ($self, $player_id = ulid)
{
	return Model::Character->new(
		player_id => $player_id,
		class_id => lore_class('Assassin')->id,
		name => $self->name->person_first_name,
	);
}

sub fake_npc ($self, $npc_id = ulid)
{
	...
}

