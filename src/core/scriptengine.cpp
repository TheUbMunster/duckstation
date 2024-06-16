#include "scriptengine.h"
#include "system.h"

#include "common/error.h"
#include "common/log.h"

#ifdef _DEBUG
#undef _DEBUG
#include "Python.h"
#define _DEBUG
#else
#include "Python.h"
#endif

Log_SetChannel(ScriptEngine);

namespace ScriptEngine {
static void SetErrorFromStatus(Error* error, const PyStatus& status, std::string_view prefix);
static void SetPyErrFromError(Error* error);

static PyObject* dspy_init();
static PyObject* dspy_is_vm_valid(PyObject* self, PyObject* args);
static PyObject* dspy_start_vm(PyObject* self, PyObject* args, PyObject* kwargs);
} // namespace ScriptEngine

void ScriptEngine::SetErrorFromStatus(Error* error, const PyStatus& status, std::string_view prefix)
{
  Error::SetStringFmt(error, "func={} err_msg={} exitcode={}", prefix, status.func ? status.func : "",
                      status.err_msg ? status.err_msg : "", status.exitcode);
}

void ScriptEngine::SetPyErrFromError(Error* error)
{
  PyErr_SetString(PyExc_RuntimeError, error ? error->GetDescription().c_str() : "unknown error");
}

bool ScriptEngine::Initialize(Error* error)
{
  PyPreConfig pre_config;
  PyPreConfig_InitIsolatedConfig(&pre_config);
  pre_config.utf8_mode = true;

  PyStatus status = Py_PreInitialize(&pre_config);
  if (PyStatus_IsError(status)) [[unlikely]]
  {
    SetErrorFromStatus(error, status, "Py_PreInitialize() failed: ");
    return false;
  }

  if (const int istatus = PyImport_AppendInittab("dspy", &dspy_init); istatus != 0)
  {
    Error::SetStringFmt(error, "PyImport_AppendInittab() failed: {}", istatus);
    return false;
  }

  PyConfig config;
  PyConfig_InitIsolatedConfig(&config);
  config.pythonpath_env = Py_DecodeLocale("C:\\Users\\Me\\AppData\\Local\\Programs\\Python\\Python311\\Lib", nullptr);

  status = Py_InitializeFromConfig(&config);

  PyMem_RawFree(config.pythonpath_env);

  if (PyStatus_IsError(status)) [[unlikely]]
  {
    SetErrorFromStatus(error, status, "Py_InitializeFromConfig() failed: ");
    return false;
  }

  return true;
}

void ScriptEngine::Shutdown()
{
  if (const int ret = Py_FinalizeEx(); ret != 0)
  {
    Log_ErrorFmt("Py_FinalizeEx() returned {}", ret);
  }
}

void ScriptEngine::EvalString(const char* str)
{
  const int res = PyRun_SimpleString(str);
  if (res == 0)
    return;

  Log_ErrorFmt("PyRun_SimpleString() returned {}", res);
  PyErr_Print();
}

#define PYBOOL(b) ((b) ? Py_NewRef(Py_True) : Py_NewRef(Py_False))

PyObject* ScriptEngine::dspy_init()
{
  static PyMethodDef s_ds_py_methods[] = {
    {"is_vm_valid", dspy_is_vm_valid, METH_NOARGS, "Returns true if a virtual machine is active."},
    {"start_vm", reinterpret_cast<PyCFunction>(dspy_start_vm), METH_VARARGS | METH_KEYWORDS,
     "Starts a new virtual machine with the specified arguments."},
    {},
  };

  static PyModuleDef s_ds_py_module = {
    PyModuleDef_HEAD_INIT, "dspy", nullptr, -1, s_ds_py_methods, nullptr, nullptr, nullptr, nullptr};

  return PyModule_Create(&s_ds_py_module);
}

PyObject* ScriptEngine::dspy_is_vm_valid(PyObject* self, PyObject* args)
{
  return PYBOOL(System::IsValid());
}

PyObject* ScriptEngine::dspy_start_vm(PyObject* self, PyObject* args, PyObject* kwargs)
{
  static constexpr const char* kwlist[] = {
    "path", "savestate", "exe", "override_fastboot", "override_slowboot", "start_fullscreen", "start_paused", nullptr};

  if (System::GetState() != System::State::Shutdown)
  {
    PyErr_SetString(PyExc_RuntimeError, "VM has already been started.");
    return nullptr;
  }

  const char* path;
  const char* savestate;
  const char* override_exe;
  int override_fastboot;
  int override_slowboot;
  int start_fullscreen;
  int start_paused;
  if (!PyArg_ParseTupleAndKeywords(args, kwargs, "$ssspppp", const_cast<char**>(kwlist), &path, &savestate,
                                   &override_exe, &override_fastboot, &override_slowboot, &start_fullscreen,
                                   &start_paused))
  {
    return nullptr;
  }

  SystemBootParameters params;
  if (path)
    params.filename = path;
  if (savestate)
    params.save_state = savestate;
  if (override_exe)
    params.override_exe = override_exe;
  if (override_fastboot)
    params.override_fast_boot = true;
  else if (override_slowboot)
    params.override_fast_boot = false;
  if (start_fullscreen)
    params.override_fullscreen = true;
  if (start_paused)
    params.override_start_paused = true;

  Error error;
  if (!System::BootSystem(std::move(params), &error))
  {
    SetPyErrFromError(&error);
    return nullptr;
  }

  return nullptr;
}
