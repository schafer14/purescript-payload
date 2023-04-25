export let webEnvironment = (typeof window !== 'undefined');
export let globalDriver = null;

export function setDriver(driver) {
    return function() {
	globalDriver = driver;
    }
}

export function getDriver() {
    return globalDriver;
}
