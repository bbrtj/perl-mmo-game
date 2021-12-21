use Test::More;

use strict;
use warnings;

use_ok('Utils');
use_ok('DI');

use_ok('Server::Worker');
use_ok('Unit');
use_ok('Game::Lore');
use_ok('Game::Ability');

# use a model to see if it registers
use_ok('Model::Player');

done_testing;
