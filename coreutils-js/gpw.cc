#include <node.h>
#include <node_events.h>
#include <assert.h>
#include <stdlib.h>
#include <sys/types.h>
#include <pwd.h>
#include <unistd.h>
#include <string.h>

using namespace node;
using namespace v8;

static Handle<Value>
GetPWNam (const Arguments& args){
  HandleScope scope;

  if (args.Length() == 0 || !args[0]->IsString()) {
    return
      ThrowException(Exception::Error(String::New("Need login name")));
  }

  char login[128];
  args[0]->ToString()->WriteAscii(login, 0, 128);

  int retval;
  struct passwd pwd_work;
  size_t pwd_buf_size = (size_t) sysconf(_SC_GETPW_R_SIZE_MAX);
  char *pwd_buf = (char *) malloc(pwd_buf_size);
  struct passwd *pwd;

  retval = getpwnam_r(login, &pwd_work, pwd_buf, pwd_buf_size, &pwd);

  Handle<Object> result = Object::New();

  if(retval != 0){
    free(pwd_buf);
    char errstr[128];
    memset(errstr, 0, 128);
    strerror_r(retval, errstr, 128);
    return
      ThrowException(Exception::Error(String::New(errstr)));
  }

  if(pwd == NULL){
    free(pwd_buf);
    return ThrowException(Exception::Error(String::New("Login not found")));
  }

  result->Set(String::NewSymbol("name"), String::New(pwd->pw_name));
  result->Set(String::NewSymbol("uid"), Integer::New(pwd->pw_uid));
  result->Set(String::NewSymbol("gid"), Integer::New(pwd->pw_gid));
  result->Set(String::NewSymbol("gecos"), String::New(pwd->pw_gecos));
  result->Set(String::NewSymbol("dir"), String::New(pwd->pw_dir));
  result->Set(String::NewSymbol("shell"), String::New(pwd->pw_shell));
  free(pwd_buf);

  return scope.Close(result);
}

static Handle<Value>
GetPWUID (const Arguments& args){
  HandleScope scope;

  if (args.Length() == 0 || !args[0]->IsUint32()) {
    return
      ThrowException(Exception::Error(String::New("Need UID")));
  }

  int uid = args[0]->Uint32Value();
  int retval;
  struct passwd pwd_work;
  size_t pwd_buf_size = (size_t) sysconf(_SC_GETPW_R_SIZE_MAX);
  char *pwd_buf = (char *) malloc(pwd_buf_size);
  struct passwd *pwd;

  retval = getpwuid_r(uid, &pwd_work, pwd_buf, pwd_buf_size, &pwd);
  Handle<Object> result = Object::New();

  if(retval != 0){
    free(pwd_buf);
    char errstr[128];
    memset(errstr, 0, 128);
    strerror_r(retval, errstr, 128);
    return
      ThrowException(Exception::Error(String::New(errstr)));
  }

  if(pwd == NULL){
    free(pwd_buf);
    return ThrowException(Exception::Error(String::New("UID not found")));
  }

  result->Set(String::NewSymbol("name"), String::New(pwd->pw_name));
  result->Set(String::NewSymbol("uid"), Integer::New(pwd->pw_uid));
  result->Set(String::NewSymbol("gid"), Integer::New(pwd->pw_gid));
  result->Set(String::NewSymbol("gecos"), String::New(pwd->pw_gecos));
  result->Set(String::NewSymbol("dir"), String::New(pwd->pw_dir));
  result->Set(String::NewSymbol("shell"), String::New(pwd->pw_shell));
  
  free(pwd_buf);

  return scope.Close(result);
}

void InitGPW(Handle<Object> target){
  HandleScope scope;

  NODE_SET_METHOD(target, "getpwnam", GetPWNam);
  NODE_SET_METHOD(target, "getpwuid", GetPWUID);
}

extern "C" void
init (Handle<Object> target) 
{
  HandleScope scope;
  InitGPW(target);
}
