use experimental 'class';

class Resource::ProjectileStop :isa(Resource);

use Game::Object::Projectile;

use header;

use constant type => 'projectile_stop';
use constant is_plaintext => true;

field $projectile :param(subject);    # Game::Object::Projectile

method generate ()
{
	return [
		$projectile->id,
	];
}

