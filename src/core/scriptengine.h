#pragma once

#include "types.h"

#include <string_view>

class Error;

namespace ScriptEngine {
bool Initialize(Error* error);
void Shutdown();

void EvalString(const char* str);
} // namespace ScriptEngine