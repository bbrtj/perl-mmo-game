package Game::Model::Player;

use header;
use Moose;
use Game::Types qw(Uuid LoreId NonEmptySimpleStr Bool DateTime Maybe);

no header;

with 'Game::Model';

has 'player' => (
	is => 'ro',
	isa => ConsumerOf ['Game::Model::Player'],
	predicate => 'is_player',
);

__PACKAGE__->_install_writers;
