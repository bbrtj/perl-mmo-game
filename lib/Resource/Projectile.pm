package Resource::Projectile;

use My::Moose;

use Game::Object::Projectile;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => InstanceOf ['Game::Object::Projectile'],
);

use constant type => 'projectile';
use constant is_plaintext => true;

sub generate ($self)
{
	my $projectile = $self->subject;

	return [
		$projectile->id,
		$projectile->effect->lore->id,
		$projectile->xy,
		$projectile->speed,
		$projectile->angle,
		$projectile->max_distance,
	];
}

