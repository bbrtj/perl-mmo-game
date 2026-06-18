use experimental 'class';

class Resource::Projectile :isa(Resource);

use Game::Object::Projectile;

use header;

use constant type => 'projectile';
use constant is_plaintext => true;

field $projectile :param(subject);    # Game::Object::Projectile

method generate ()
{
	return [
		$projectile->id,
		$projectile->effect->lore->id,
		$projectile->xy,
		$projectile->speed,
		$projectile->angle,
		$projectile->max_distance,
	];
}

