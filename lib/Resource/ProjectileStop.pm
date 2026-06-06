package Resource::ProjectileStop;

use My::Moose;

use Game::Object::Projectile;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => InstanceOf ['Game::Object::Projectile'],
);

use constant type => 'projectile_stop';
use constant is_plaintext => true;

sub generate ($self)
{
	my $projectile = $self->subject;

	return [
		$projectile->id,
	];
}

