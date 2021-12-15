use Test::More;
use Game::Model::User;

use header -noclean;

my $dummy = Game::Model::User->dummy->new;
isa_ok $dummy, 'Game::Model::User::Dummy';

my $password = 'aoeuaoeu1';
$dummy->set_email('brtastic.dev@gmail.com');
$dummy->set_password($password);
$dummy->promote;

isa_ok $dummy, 'Game::Model::User';
ok $dummy->verify_password($password), 'password verification ok';
ok !$dummy->verify_password($password . 'x'), 'wrong password verification fail ok';
isnt $dummy->password, $password, 'password hashed ok';

done_testing;

