use Model::User;

use testheader;

my $dummy = Model::User->new;

my $password = 'aoeuaoeu1';
$dummy->set_email('a@gmail.com');
$dummy->set_password($password);

$dummy->promote;
$dummy->set_email('brtastic.dev@gmail.com');

ok $dummy->meta->is_immutable, 'model is moose-immutable';

isa_ok $dummy, 'Model::User';
is $dummy->email, 'brtastic.dev@gmail.com', 'email ok';
ok $dummy->verify_password($password), 'password verification ok';
ok !$dummy->verify_password($password . 'x'), 'wrong password verification fail ok';
isnt $dummy->password, $password, 'password hashed ok';

done_testing;

