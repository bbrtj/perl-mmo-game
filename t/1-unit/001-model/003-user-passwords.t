use all 'Model';

use testheader;

my $password = 'password!';

my $model = Model::User->new(
	email => 'test@test.com',
	plaintext_password => $password
);

ok $model->verify_password(hash_password($password)), 'password verified ok';

done_testing;

