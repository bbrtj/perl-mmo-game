use all 'Model';

use testheader;

my $dummy = Model::User->dummy;

my $password = 'aoeuaoeu1';
$dummy->set_email('a@gmail.com');
$dummy->set_password($password);

$password = hash_password($password);

$dummy->set_email('brtastic.dev@gmail.com');

isa_ok $dummy, 'Model::User';
is length $dummy->id, 26, 'ulid ok';
is $dummy->email, 'brtastic.dev@gmail.com', 'email ok';
ok $dummy->verify_password($password), 'password verification ok';
ok !$dummy->verify_password($password . 'x'), 'wrong password verification fail ok';
isnt $dummy->password, $password, 'password hashed ok';

done_testing;

