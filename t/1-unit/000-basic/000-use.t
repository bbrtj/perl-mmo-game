use Test::More;

use strict;
use warnings;

use_ok('Game::Common');
use_ok('DI');

use_ok('Game::Worker');
use_ok('Game::Unit');
use_ok('Game::Lore');
use_ok('Game::Ability');

# use a model to see if it registers
use_ok('Game::Model::Player');

done_testing;