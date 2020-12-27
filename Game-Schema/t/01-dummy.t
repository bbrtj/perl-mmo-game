use v5.32;
use warnings;

use Test::More;
use Game::Model::User;

my $dummy = Game::Model::User->dummy->new;
isa_ok $dummy, 'Game::Model::User::Dummy';

my $password = 'aoeuaoeu1';
$dummy->set_email('brtastic.dev@gmail.com');
$dummy->set_password($password);
$dummy->promote;

isa_ok $dummy, 'Game::Model::User';
ok $dummy->verify_password($password);
ok !$dummy->verify_password($password . 'x');
isnt $dummy->password, $password;

done_testing;

